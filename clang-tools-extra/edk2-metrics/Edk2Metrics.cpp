// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

#include "clang/AST/ASTContext.h"

using namespace clang::tooling;
using namespace llvm;
using namespace clang;
using namespace clang::ast_matchers;

StatementMatcher LoopMatcher =
    forStmt(hasLoopInit(declStmt(
                hasSingleDecl(varDecl(hasInitializer(integerLiteral(equals(0))))
                                  .bind("initVarName")))),
            hasIncrement(unaryOperator(
                hasOperatorName("++"),
                hasUnaryOperand(declRefExpr(
                    to(varDecl(hasType(isInteger())).bind("incVarName")))))),
            hasCondition(binaryOperator(
                hasOperatorName("<"),
                hasLHS(ignoringParenImpCasts(declRefExpr(
                    to(varDecl(hasType(isInteger())).bind("condVarName"))))),
                hasRHS(expr(hasType(isInteger())))))).bind("forLoop");

class LoopPrinter : public MatchFinder::MatchCallback {
public :
  virtual void run(const MatchFinder::MatchResult &Result);
  // virtual void run(const MatchFinder::MatchResult &Result) {
    // if (const ForStmt *FS = Result.Nodes.getNodeAs<clang::ForStmt>("forLoop"))
      // FS->dump();
  // }
};

static bool areSameVariable(const ValueDecl *First, const ValueDecl *Second) {
  return First && Second &&
         First->getCanonicalDecl() == Second->getCanonicalDecl();
}

void LoopPrinter::run(const MatchFinder::MatchResult &Result) {
  ASTContext *Context = Result.Context;
  const ForStmt *FS = Result.Nodes.getNodeAs<ForStmt>("forLoop");
  // We do not want to convert header files!
  if (!FS || !Context->getSourceManager().isWrittenInMainFile(FS->getForLoc()))
    return;
  const VarDecl *IncVar = Result.Nodes.getNodeAs<VarDecl>("incVarName");
  const VarDecl *CondVar = Result.Nodes.getNodeAs<VarDecl>("condVarName");
  const VarDecl *InitVar = Result.Nodes.getNodeAs<VarDecl>("initVarName");

  if (!areSameVariable(IncVar, CondVar) || !areSameVariable(IncVar, InitVar))
    return;
  llvm::outs() << "Potential array-based loop discovered.\n";
}

/////////////////////////////////////////////////////////////////////////////
DeclarationMatcher FunDeclBodyMatcher = functionDecl(hasBody(compoundStmt())).bind("funDeclBody");
DeclarationMatcher FunDeclMatcher = functionDecl().bind("funDecl");
StatementMatcher   CalleeMatcher = callExpr().bind("callee");


class FunctionPrinter : public MatchFinder::MatchCallback {
public :
  virtual void run(const MatchFinder::MatchResult &Result){

    static int count = 0;
    llvm::outs() << "FunctionPrinter is called:" << count++ << "times \n";

    if (const FunctionDecl *FD = Result.Nodes.getNodeAs<FunctionDecl>("funDecl")) {
      const auto& SM = *Result.SourceManager;
      const auto& Loc = FD->getLocation();
      llvm::outs() << SM.getFilename(Loc) << ":"
                   << FD->getNameInfo().getAsString() << ":"
                   << SM.getSpellingLineNumber(Loc) << ":"
                   << SM.getSpellingColumnNumber(Loc) << "\n";
      if (FD->hasBody()){
        FD->dump();
      }
    }

    // if (const auto  *FD = Result.Nodes.getNodeAs<clang::FunctionDecl>("funDeclBody")) {
      // const auto& SM = *Result.SourceManager;
      // const auto& Loc = FD->getLocation();
      // llvm::outs() << SM.getFilename(Loc) << ":"
                   // << SM.getSpellingLineNumber(Loc) << ":"
                   // << SM.getSpellingColumnNumber(Loc) << "\n";
      // FD->dump();
    // }

    if (const CallExpr *CE = Result.Nodes.getNodeAs<CallExpr>("callee")) {
      const auto& SM = *Result.SourceManager;
      const auto& Loc = CE->getBeginLoc();

      const FunctionDecl *FD = CE->getDirectCallee();
      if (FD) {
        llvm::outs() << SM.getFilename(Loc) << ":"
                     << FD->getNameInfo().getAsString() << ":"
                     << SM.getSpellingLineNumber(Loc) << ":"
                     << SM.getSpellingColumnNumber(Loc) << "\n";
      }else{
        // Function pointer
        llvm::outs() << SM.getFilename(Loc) << ":"
                     << SM.getSpellingLineNumber(Loc) << ":"
                     << SM.getSpellingColumnNumber(Loc) << "\n";
      }
      CE->dump();
    }
  }
};


// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory MyToolCategory("my-tool options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...\n");

int main(int argc, const char **argv) {
  CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  //LoopPrinter Printer;
  FunctionPrinter FunPrinter;
  MatchFinder Finder;
  //Finder.addMatcher(LoopMatcher, &Printer);
  //Finder.addMatcher(FunDeclBodyMatcher, &FunPrinter);
  Finder.addMatcher(FunDeclMatcher, &FunPrinter);
  Finder.addMatcher(CalleeMatcher, &FunPrinter);

  return Tool.run(newFrontendActionFactory(&Finder).get());
}
