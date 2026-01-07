# Mar.datawrangling NEWS

## Mar.datawrangling 2.0.3

Stomach data ("STOMACH_DATA_VW") is now one of the included data tables.

## Mar.datawrangling 2.0.0

### BREAKING CHANGES

This release contains significant breaking changes that will impact all users. Please read carefully and update your code accordingly.

#### Oracle Connection Changes

* **REMOVED**: `username`, `password`, `dsn` and `usepkg` parameters from all functions
* **NEW REQUIREMENT**: Pass an existing, valid Oracle connection via `cxn` parameter
* **WHY**: Improves troubleshooting by separating connection issues from package issues
* **MIGRATION**: 
  ```r
  # Old way
  get_data('rv', username = "myuser", password = "mypass", dsn=`PTRAN`, usepkg="roracle")
  
  # New way  
  cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), "<oracle.username>", "<oracle.password>"", "PTRAN")
  get_data('rv', cxn = cxn)
  ```

#### Data Storage Changes

* **REMOVED**: `data.dir` parameter - extractions now go to standardized folders within `C:\DFO-MPO`
* **NEW**: Protected B RData files are now encrypted when extracted
* **IMPACT**: Cannot easily share extractions with other users; cannot load others' extractions without detailed knowledge of original extractor
* **WHY**: 
  - Prevents accidental data duplication and storage waste
  - Ensures proper handling of Protected B data after discovering unprotected extractions on network drives
  - Improves data management and security compliance

#### What You Need to Do

1. **Update your scripts** to establish Oracle connections before calling package functions
2. **Remove** any `data.dir` parameter usage from your code
3. **Re-extract data** using the new system (old extractions may not be compatible)
4. **Do not share** raw extraction files with other users

### Technical Details

* Valid Oracle connections must be passed to certain functions.  ROracle, RODBC, DBI and JDBC connections should all work 
* Data extractions are stored in user-specific encrypted format
* Underlying RData files can still be copied for archival purposes, but should be done intentionally and rarely
* All extraction and filtering functionality remains the same once connection is established

### Need Help?

* Check the updated README.md for migration examples
* Contact mike.mcmahon@dfo-mpo.gc.ca for assistance
* Report issues at https://github.com/Maritimes/Mar.datawrangling/issues
