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
