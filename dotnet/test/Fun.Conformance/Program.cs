// Runs the shared conformance suite -- the cases/<area>/<name>.fun files under
// test/conformance/cases, compared against their sibling <name>.expect. It is the only place
// a language behaviour is tested, and it began life shared with the OCaml prototype (removed
// 2026-09-25), which is why it lives beside the case files rather than under dotnet/.
// See ../../../test/conformance/cases/README.md.
using Fun.Compiler;

const string UnitInfix = ".unit-";

// --file <path>: run a single program and print the one-line protocol below. It reuses
// the same value/error shape as a normal case, so a program is judged exactly as the
// suite judges it -- the way to check a suspected bug without adding a case first.
if (args is ["--file", var file])
    return RunFile(file);

// prototype-divergences.txt is historical now (the second implementation is gone); the
// cases it lists are ordinary ones, and this runner passes every case in the directory.
var root = CasesRoot(args);
var cases = Directory.EnumerateDirectories(root)
    .OrderBy(d => d, StringComparer.Ordinal)
    .SelectMany(area => Directory.EnumerateFiles(area, "*.fun")
        .Where(f => !Path.GetFileName(f).Contains(UnitInfix, StringComparison.Ordinal))
        .OrderBy(f => f, StringComparer.Ordinal))
    .ToList();

var failures = cases
    .Select(path => (path, why: RunCase(path)))
    .Where(r => r.why is not null)
    .ToList();

foreach (var (path, why) in failures)
    Console.WriteLine($"FAIL {Path.GetRelativePath(root, path)}: {why}");
Console.WriteLine($"conformance: {cases.Count} cases, {failures.Count} failed");
return failures.Count == 0 ? 0 : 1;

// null when the case passes, otherwise why it did not.
static string? RunCase(string path)
{
    var dir = Path.GetDirectoryName(path)!;
    var name = Path.GetFileNameWithoutExtension(path);
    var expect = File.ReadAllText(Path.Combine(dir, name + ".expect")).Trim();
    var units = UnitSources(dir, name);

    Elaborated elaborated;
    try
    {
        elaborated = Driver.Elaborate(File.ReadAllText(path), units);
    }
    // A path still to be ported fails the case; it never passes one expecting `error`.
    catch (NotImplementedException e) { return e.Message; }
    catch (FunException e)
    {
        return expect == "error" ? null : $"elaboration failed: {e.Message}";
    }
    // An invariant failure reports as this case's failure, not as a crash of the run.
    // One reachable path the port believes is unreachable must not hide the other
    // 705 results - and it still never passes a case expecting `error`.
    // A UnifyException that escapes is one of those: a type mismatch is a
    // FunException by the time it surfaces, so an escaped one is an invariant
    // failure, never a language error.
    catch (Exception e) when (e is InvalidOperationException or IndexOutOfRangeException or ArgumentException or UnifyException)
    {
        return $"invariant failure ({e.GetType().Name}): {e.Message}";
    }

    if (expect == "ok") return null;

    // An `error` case that elaborates is run too: it may fail at evaluation
    // (cases/README.md). Running has no budget, so a run that does not finish is
    // reported rather than left to hang the suite.
    // ponytail: a timed-out run's thread keeps spinning until the process exits.
    var timeout = TimeSpan.FromSeconds(10);
    var run = Task.Run(() =>
    {
        try { return (Value: (string?)Driver.Describe(Driver.Run(elaborated)), Error: (Exception?)null); }
        catch (Exception e) when (e is FunException or NotImplementedException
                                      or InvalidOperationException or UnifyException) { return (null, e); }
    });
    if (!run.Wait(timeout)) return $"did not finish within {timeout.TotalSeconds}s";
    var (got, error) = run.Result;

    return (expect, error) switch
    {
        (_, NotImplementedException e) => e.Message,
        (_, InvalidOperationException e) => $"invariant failure ({e.GetType().Name}): {e.Message}",
        (_, UnifyException e) => $"invariant failure ({e.GetType().Name}): {e.Message}",
        ("error", FunException) => null,
        ("error", _) => "expected an error",
        (_, FunException e) => $"evaluation failed: {e.Message}",
        _ => got == expect ? null : $"expected {expect}, got {got}",
    };
}

// A case's extra compilation units: <name>.unit-<unit>.fun beside it, each
// importable as "<unit>".
static Dictionary<string, string> UnitSources(string dir, string name)
{
    var prefix = name + UnitInfix;
    return Directory.EnumerateFiles(dir, prefix + "*.fun")
        .ToDictionary(
            f => Path.GetFileNameWithoutExtension(f)[prefix.Length..],
            File.ReadAllText,
            StringComparer.Ordinal);
}

// test/conformance/cases, found by walking up to the repo root, so the runner works from
// anywhere. An argument overrides it.
static string CasesRoot(string[] args)
{
    if (args.Length > 0) return args[0];
    // Walk up to the repo root. The marker is the cases directory itself, not a build file:
    // the runner used to look for `dune-project`, which the prototype's removal took with it.
    for (var dir = AppContext.BaseDirectory; dir is not null; dir = Path.GetDirectoryName(dir))
    {
        var cases = Path.Combine(dir, "test", "conformance", "cases");
        if (Directory.Exists(cases)) return cases;
    }
    throw new DirectoryNotFoundException("no test/conformance/cases above the runner; pass the cases directory");
}

// The probe protocol, one line per program (the `--file` mode above):
//   OK           the program elaborates (sibling .expect is "ok", so it is not run)
//   VALUE <s>    Driver.Describe of the result
//   ELAB <msg>   expansion/elaboration failed
//   EVAL <msg>   evaluation failed
//   HANG         the run did not finish within 60s
// The accounting stays honest: an unported path is a failure, an invariant
// failure is a failure, and neither passes a program expecting `error`.
static int RunFile(string path)
{
    var full = Path.GetFullPath(path);
    var dir = Path.GetDirectoryName(full)!;
    var name = Path.GetFileNameWithoutExtension(full);
    var units = UnitSources(dir, name);
    var expect = Path.Combine(dir, name + ".expect");
    var elaboratesOnly = File.Exists(expect) && File.ReadAllText(expect).Trim() == "ok";

    Elaborated elaborated;
    try
    {
        elaborated = Driver.Elaborate(File.ReadAllText(full), units);
    }
    catch (Exception e)
    {
        PrintFile("ELAB", DescribeFailure(e));
        return 0;
    }

    if (elaboratesOnly)
    {
        PrintFile("OK", "");
        return 0;
    }

    var timeout = TimeSpan.FromSeconds(60);
    var run = Task.Run(() =>
    {
        try { return (Value: (string?)Driver.Describe(Driver.Run(elaborated)), Error: (Exception?)null); }
        catch (Exception e) { return (Value: (string?)null, Error: e); }
    });
    if (!run.Wait(timeout))
    {
        PrintFile("HANG", "");
        return 0;
    }
    var (value, error) = run.Result;
    if (error is not null)
    {
        PrintFile("EVAL", DescribeFailure(error));
        return 0;
    }
    PrintFile("VALUE", value ?? "");
    return 0;
}

// The honest failure description for --file: an unported path says so, an
// invariant failure says so, anything unexpected names its type.
static string DescribeFailure(Exception e) => e switch
{
    NotImplementedException => "not ported: " + e.Message,
    FunException => e.Message,
    InvalidOperationException or IndexOutOfRangeException or ArgumentException or UnifyException =>
        $"invariant ({e.GetType().Name}): {e.Message}",
    _ => $"{e.GetType().Name}: {e.Message}",
};

static void PrintFile(string tag, string msg) =>
    Console.WriteLine(msg.Length == 0 ? tag : $"{tag} {OneLine(msg)}");

static string OneLine(string s) => s.Replace('\r', ' ').Replace('\n', ' ');
