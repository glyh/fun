namespace Quill.Tests;

/// <summary>
/// The conformance runner's classification of a case's exception
/// (<c>CaseJudge</c> in test/Quill.Conformance/Program.cs). Pinned so that an engine
/// invariant broken by a mutation -- or by a regression -- reports as a hard
/// failure naming its type and message: never aborting the suite (a sweep loses
/// every other case's result when it does), never passing a case expecting `error`.
/// The suite-wide proof is the sweep itself; this is the one check that fails if
/// the mapping regresses.
/// </summary>
public class ConformanceJudgeTests
{
    [Fact]
    public void AnUnexpectedExceptionTypeIsAHardFailureThatNeverPasses()
    {
        var reason = CaseJudge.Failure(new NullReferenceException("boom"), "error", "evaluation");
        Assert.NotNull(reason);
        Assert.Contains("hard failure", reason);
        Assert.Contains("NullReferenceException", reason);
        Assert.Contains("boom", reason);
    }
}
