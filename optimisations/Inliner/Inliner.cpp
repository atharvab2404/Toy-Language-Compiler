#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/InlinerPass.h"

using namespace llvm;
namespace {

class Inliner : public Inliner {
  InlineCostAnalysis *ICA;

public:
    Inliner()
      : Inliner(ID, -2000000000,
                 true),
        ICA(nullptr) {
    initializeInlinerPass(*PassRegistry::getPassRegistry());
  }

  Inliner(bool InsertLifetime)
      : Inliner(ID, -2000000000, InsertLifetime), ICA(nullptr) {
    initializeInlinerPass(*PassRegistry::getPassRegistry());
  }

  static char ID;

  InlineCost getInlineCost(CallSite CS) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override;
  bool runOnSCC(CallGraphSCC &SCC) override;

  using llvm::Pass::doFinalization;
  bool doFinalization(CallGraph &CG) override {
    return removeDeadFunctions(CG,true);
  }
};
} 

char Inliner::ID = 0;
INITIALIZE_PASS_BEGIN(Inliner, "my-inline", "Inliner for my_inline functions",false, false)
INITIALIZE_AG_DEPENDENCY(AliasAnalysis)
INITIALIZE_PASS_DEPENDENCY(AssumptionCacheTracker)
INITIALIZE_PASS_DEPENDENCY(CallGraphWrapperPass)
INITIALIZE_PASS_DEPENDENCY(InlineCostAnalysis)
INITIALIZE_PASS_END(Inliner, "my-inline",
                    "Inliner for always_inline functions", false, false)

Pass *llvm::createMyInlinerPass() { return new MyInliner(); }

Pass *llvm::createMyInlinerPass(bool InsertLifetime) {
  return new Inliner(InsertLifetime);
}

InlineCost Inliner::getInlineCost(CallSite CS) {
  Function *Callee = CS.getCalledFunction();
  if (Callee && !Callee->isDeclaration() &&
      CS.hasFnAttr(Attribute::AlwaysInline) && ICA->isInlineViable(*Callee))
    return InlineCost::getAlways();

  return InlineCost::getNever();
}

bool Inliner::runOnSCC(CallGraphSCC &SCC) {
  ICA = &getAnalysis<InlineCostAnalysis>();
  return Inliner::runOnSCC(SCC);
}

void Inliner::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<InlineCostAnalysis>();
  Inliner::getAnalysisUsage(AU);
}