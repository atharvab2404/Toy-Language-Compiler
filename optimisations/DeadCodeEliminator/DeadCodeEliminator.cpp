#include "llvm/Transforms/Scalar.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Pass.h"

using namespace llvm;
#define DEBUG_TYPE "DeadCodeEliminator"
namespace {
    struct DeadCodeEliminator : public FunctionPass {
        static char ID;
        DeadCodeEliminator() : FunctionPass(ID){
            initializeMYADCEPass(*PassRegistry::getPassRegistry());
        }

        bool runOnFunction (Function &F) override {
            if (skipFunction(F))
                return false;

            std::set<Instruction*> Alive;
            std::vector<Instruction*> Worklist;
            Function *Func = &F;

            for (inst_iterator i = inst_begin(Func), e = inst_end(Func); i != e; ++i) {
                Instruction *I = &*i;
                if (isa<TerminatorInst>(I) || isa<DbgInfoIntrinsic>(I)
                    || isa<LandingPadInst>(I) || I->mayHaveSideEffects()) {
                    Alive.insert(I);
                    Worklist.push_back(I);
                }
            }

            while (!Worklist.empty()) {
                Instruction *Curr = Worklist.back();
                Worklist.pop_back();
                for (Use &OI : Curr->operands()) {
                    if (Instruction *Inst = dyn_cast<Instruction>(OI))
                        if (Alive.insert(Inst).second)
                            Worklist.push_back(Inst);
                }
            }

            for (inst_iterator i = inst_begin(Func), e = inst_end(Func); i != e; ++i) {
                Instruction *I = &*i;
                if (!Alive.count(I)) {
                    Worklist.push_back(I);
                    I->dropAllReferences();
                }
            }

            for (Instruction *&I : Worklist) {
                I->eraseFromParent();
            }
            return !Worklist.empty();
        }
        virtual void getAnalysisUsage(AnalysisUsage& AU) const override {
            AU.setPreservesCFG();
        }
    };
}
Pass *llvm::createMyADCEPass() { 
    return new DeadCodeEliminator(); 
}
char MYADCE::ID = 0;
INITIALIZE_PASS(DeadCodeEliminator, "DeadCodeEliminator", "Dead Code Elimination Pass", false, false)