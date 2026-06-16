| From table | From key | To table | To key | Relationship role |
|---|---|---|---|---|
| `tblStation` | `KeyStation` | `tblStationSubGear` | `KeyStation` | One station has one subgear |
| `tblStationSubGear` | `KeyStationSubGear` | `tblCatch` | `KeyStationSubGear` | One subgear has catch |
| `tblCatch` | `KeyCatch` | `tblCatchSub1` | `KeyCatch` | One catch has one `tblCatchSub1` |
| `tblCatchSub1` | `KeyCatchSub1` | `tblCatchSub2` | `KeyCatchSub1` | One `tblCatchSub1` has one `tblCatchSub2` |
| `tblCatchSub2` | `KeyCatchSub2` | `tblIndividual` | `KeyCatchSub2` | One `tblCatchSub2` has many entries in `tblIndividual` |
| `tblIndividual` | `KeyIndividual` | `tblIndividualMeasure` | `KeyIndividual` | One entry in `tblIndividual` might have multiple entries in `tblIndividualMeasure` |
| `tblCatchSub2` | `Species` | `tblLstSpecies` | `Species` | Species code links catch records to species lookup |

## Observed `tblIndividualMeasure` Columns

| Column | Meaning in these queries | Observed values / pattern |
|---|---|---|
| `KeyIndividual` | Links a measurement row to one row in `tblIndividual` | Join key |
| `IndividualMeasureType` | Tells you what kind of measurement the row represents | `ShrimpSex`, `ShrimpRoe`, `EggHair` |
| `Attribute` | Stores the categorical value for that measurement type | For `ShrimpSex`: `FP`, `M`; for `ShrimpRoe`: `None`; for `EggHair`: `0` |
| `Measure` | Numeric or free-form measured value | Selected in `select-all-males.sql`, but no concrete values are constrained in the SQL files here |

## Observed `tblStation` Columns

| Column | Meaning in these queries | Observed values / pattern |
|---|---|---|
| `KeyStation` | Primary key used to connect `tblStation` to `tblStationSubGear` | Join key |
| `TripYear` | Filters the year for the station/trip | `2026` |
| `Ship` | Filters the vessel code | `TA` |
| `Trip` | Filters the trip number or code | `2` |
| `Station` | Filters the station number | `50` |
