// Runs the shared conformance suite -- the same cases/<area>/<name>.fun files
// the OCaml prototype runs, compared against the same <name>.expect. The port
// is complete when this passes every case OCaml passes.
// See ../../../test/conformance/cases/README.md.
using Fun.Compiler;

const string UnitInfix = ".unit-";

// prototype-divergences.txt is the OCaml runner's business: the port passes every case.
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

    if (expect == "error") return "expected an error";
    if (expect == "ok") return null;

    try
    {
        var got = Driver.Describe(Driver.Run(elaborated));
        return got == expect ? null : $"expected {expect}, got {got}";
    }
    catch (NotImplementedException e) { return e.Message; }
    catch (FunException e)
    {
        return $"evaluation failed: {e.Message}";
    }
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

// test/conformance/cases, found by walking up to the repo root (dune-project),
// so the runner works from anywhere. An argument overrides it.
static string CasesRoot(string[] args)
{
    if (args.Length > 0) return args[0];
    for (var dir = AppContext.BaseDirectory; dir is not null; dir = Path.GetDirectoryName(dir))
    {
        if (!File.Exists(Path.Combine(dir, "dune-project"))) continue;
        return Path.Combine(dir, "test", "conformance", "cases");
    }
    throw new DirectoryNotFoundException("no dune-project above the runner; pass the cases directory");
}
