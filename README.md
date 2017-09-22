# genesis-auditor

# Example Usage

When given a correct genesis file, it will spit out a bunch of checks passed:

```
$ stack exec -- genesis-auditor --target-sha <blake2b_256_sha> -s stakeholders.txt -i mainnet-genesis-dryrun-with-stakeholders.json
is-canonical-json-check -> Check Passed
delegation-stakeholders-match -> Check Passed
delegation-address-correspondence -> Check Passed
delegation-vss-correspondence -> Check Passed
```

When something is not correct, a failure will be reported:

```
$ stack exec -- genesis-auditor --target-sha <blake2b_256_sha> -s fake_stakeholders.txt -i mainnet-genesis-dryrun-with-stakeholders.json
is-canonical-json-check -> Check Passed
delegation-stakeholders-match -> Check Failed: Expecting stakeholders fromList ["a", "b", "c", "d"]
  missing stakeholders: fromList ["a"]
  unexpected stakeholders: fromList ["e"]
delegation-address-correspondence -> Check Passed
delegation-vss-correspondence -> Check Passed
```

# Implemented Checks

The asserts the following properties of the genesis data:

- [ ] The `avvmDistr` matches the output from the AVVM
- [x] the `nonAvvmBalances` is empty
- [x] The stakeholder addresses in the json file are exactly those provided in the stakeholders file
- [x] For each stakeholder address, there is a delegation certificate.
- [x] There are no delegation certificates that do not belong to one of the stakeholders
- [x] Each of the `delegatePk` from the delegation certificates corresponds to a `signingKey` in a vss certificate
- [x] There are no vss certificates where the signing key is not used as a `delegatePk` in one of the delegation certificates
- [x] Check that the number of stakeholders and the number of vss certificates are the same.  This could be violated if multiple stakeholders delegated to the same core node
- [ ] Converting to canonical JSON form and taking the `blake2b_256_sha` results in the specified hash.
- [ ] Consistency checks on the `vssCerts`
- [ ] Check that there are no duplicate JSON entries in the input
