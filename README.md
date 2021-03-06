# genesis-auditor

# Example Usage

When given a correct genesis file, it will spit out a bunch of checks passed:

```
$ stack exec -- genesis-auditor --target-sha <blake2b_256_sha> -s stakeholders.txt -i mainnet-genesis-dryrun-with-stakeholders.json -c vss-certs.txt -a inputGenesis.json
is-canonical-json-check -> Check Passed
delegation-stakeholders-match -> Check Passed
delegation-address-correspondence -> Check Passed
delegation-vss-correspondence -> Check Passed
```

When something is not correct, a failure will be reported:

```
$ stack exec -- genesis-auditor --target-sha <blake2b_256_sha> -s fake_stakeholders.txt -i mainnet-genesis-dryrun-with-stakeholders.json -c vss-certs.txt -a inputGenesis.json
is-canonical-json-check -> Check Passed
delegation-stakeholders-match -> Check Failed: Expecting stakeholders fromList ["a", "b", "c", "d"]
  missing stakeholders: fromList ["a"]
  unexpected stakeholders: fromList ["e"]
delegation-address-correspondence -> Check Passed
delegation-vss-correspondence -> Check Passed
```

# Implemented Checks

The asserts the following properties of the genesis data:

- [X] The `avvmDistr` matches the output from the AVVM
- [X] the `nonAvvmBalances` is empty
- [X] The stakeholder addresses in the json file are exactly those provided in the stakeholders file
- [X] For each stakeholder address, there is a delegation certificate.
- [X] There are no delegation certificates that do not belong to one of the stakeholders
- [X] Each of the `delegatePk` from the delegation certificates corresponds to a `signingKey` in a vss certificate
- [X] There are no vss certificates where the signing key is not used as a `delegatePk` in one of the delegation certificates
- [X] Check that the number of stakeholders and the number of vss certificates are the same.  This could be violated if multiple stakeholders delegated to the same core node
- [X] Converting to canonical JSON form and taking the `blake2b_256_sha` results in the specified hash.
- [X] Consistency checks on the `vssCerts`
- [X] Check that there are no duplicate JSON entries in the input
