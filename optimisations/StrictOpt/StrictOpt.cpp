#include "StrictOpt.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

PreservedAnalyses StrictOpt::run(Function &F, FunctionAnalysisManager &FAM) {
  bool Modified = false;
  for (auto &Arg : F.args()) {
    if (Arg.getType()->isPointerTy() && !Arg.hasAttribute(Attribute::NoAlias)) {
      Arg.addAttr(Attribute::NoAlias);
      Modified |= true;
    }
  }

  auto PA = PreservedAnalyses::all();
  if (Modified)
    PA.abandon<AAManager>();

  return PA;
}

extern "C" ::llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK
llvmGetPassPluginInfo() {
  return {
    LLVM_PLUGIN_API_VERSION, "StrictOpt", "v0.1",
    [](PassBuilder &PB) {
#ifdef STRICT_OPT_USE_PIPELINE_PARSER
      using PipelineElement = typename PassBuilder::PipelineElement;
      PB.registerPipelineParsingCallback(
        [](StringRef Name, FunctionPassManager &FPM, ArrayRef<PipelineElement>){
          if (Name == "strict-opt") {
            FPM.addPass(StrictOpt());
            return true;
          }
          return false;
        });
#else
      using OptimizationLevel= typename PassBuilder::OptimizationLevel;
      PB.registerPipelineStartEPCallback(
        [](ModulePassManager &MPM, OptimizationLevel OL) {
          if (OL.getSpeedupLevel() >= 2)
            MPM.addPass(createModuleToFunctionPassAdaptor(StrictOpt()));
        });
#endif
    }
  };
}