{ pkgs, lib }:

rec {

  flattenDerivationTree = separator: set:
    let
      recurse = name: name':
        flatten (if name == "" then name' else "${name}${separator}${name'}");

      flatten = name': value:
        let
          name = builtins.replaceStrings [ ":" ] [ separator ] name';
        in
        if lib.isDerivation value || lib.typeOf value != "set" then
          [{ inherit name value; }]
        else
          lib.concatLists (lib.mapAttrsToList (recurse name) value);
    in
    assert lib.typeOf set == "set";
    lib.listToAttrs (flatten "" set);


  mapAttrsValues = f: lib.mapAttrs (_name: f);


  # Collect all derivations in a job tree, skipping test runs, i.e. `checks`
  # attributes at any depth.
  collectDerivationsWithoutChecks =
    let
      go = value:
        if lib.isDerivation value then
          [ value ]
        else if lib.isAttrs value then
          lib.concatLists
            (lib.mapAttrsToList (_: go)
              (removeAttrs value [ "checks" "recurseForDerivations" ]))
        else
          [ ];
    in
    go;


  makeHydraRequiredJob = hydraJobs:
    let
      cleanJobs = lib.filterAttrsRecursive
        (name: _: name != "recurseForDerivations")
        (removeAttrs hydraJobs [ "required" ]);
    in
    pkgs.releaseTools.aggregate {
      name = "required";
      meta.description = "All jobs required to pass CI";
      constituents = lib.collect lib.isDerivation cleanJobs;
    };
}
