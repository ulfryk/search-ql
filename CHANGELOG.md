# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## Unreleased

### Fix
- preserve const enums

## 8.0.1 - 2018-10-19
### Fix
- fix `isDate` function

## 8.0.0 - 2018-10-16
### Changed
- **[breaking]**  move date and number testing and parsing to `common/utils` and re-export them in main index
- `timeFrame` field added to `DateExpression` and `IDateExpression`
### Removed
- **[breaking]** preparedValue dropped from TermExpressions and DTO

## 7.0.1 - 2018-10-11
### Fix
- drop fancy functions operating on TEXT type values (unused and not needed):
  - `​​remove_stop_words`
  - `​​cleanup`

## 7.0.0 - 2018-10-09
### Added
- **[breaking]** function aliases
- **[breaking]** generic functions
- functions operating on ANY type values:
  - `coalesce`
  - `is_date`
  - `is_number`
  - `typeof`
- functions operating on TEXT type values:
  - `​​concat`
  - `​​trim`
  - `​​lower`
  - `​​upper`
  - `​​remove_stop_words`
  - `​​cleanup`
- functions operating on numeric (DATE / NUMBER) type values:
  - `max`
  - `min`
- functions operating on DATE type values:
  - `​​days_ago`
  - `​​months_ago`
  - `​​years_ago`
  - `​​days_diff`
  - `​​months_diff`
  - `​​years_diff`
  - `​​now`
- functions operating on NUMBER type values:
  - `abs`
  - `​​round`
  - `​​ceil`
  - `​​floor`

## 6.0.0 - 2018-09-25
### Changed
- **[breaking]** changed `Failure` classes
- **[breaking]** AST validation API
### Added
- integrity check
### Fixed
- type checker
- update dependencies

## 5.0.1 - 2018-09-20
### Fixed
- fix typings and type dependencies

## 5.0.0 - 2018-09-20
### Removed
- **[breaking]** - testers completely moved out from library
### Changed
- **[breaking]** changed API of `SearchQLParser` - `toTester` method removed
### Fixed
- update dependencies

## 4.0.0 - 2018-09-19
### Added
- `GT`, `GTE`, `LT` and `LTE` relational (ord) binary operators
- `NOT LIKE` similarity binary operator

### Changed
- **[breaking]** changed entry point - `SearchQLParser` class instead of `parseSearchQL` function
### Fixed
- update dependencies

## 3.0.0 - 2018-09-15
### Added
- allow custom configuration
- operators and binary operation expressions
  - `AND`, `OR` logical operators (used with `BinaryOperationExpression`)
  - `LIKE` similarity operator (replacement for `LabelledExpression`)
  - `IS`, `IS NOT` equality operators (used with `BinaryOperationExpression`)
  - `NOT` - (used with `NotExpression`, `UnaryOperationExpression` will be implemented soon)
- abstract `TermExpression` with sub expressions:
  - `DateExpression`
  - `NumberExpression`
  - `SelectorExpression` (used with binary operator `LIKE` to replace `LabelledExpression`)
  - `TextExpression`
  - `PhraseExpression` (replaces `BasicExpression`)
- operators precedence and associativity
- **[breaking]** type checking
### Removed
- **[breaking]** drop `LabelledExpression`
- **[breaking]** drop `BasicExpression`
- **[breaking]** remove `test` method from expression
### Changed
- **[breaking]** don't ignore trailing operator
- **[breaking]** input testing decoupled from AST
### Fixed
- update dependencies

## 2.1.3 - 2018-08-02
### Fixed
- update dependencies

## 2.1.2 - 2018-07-11
### Fixed
- update dependencies

## 2.1.1 - 2018-05-11
### Fixed
- update dependencies

## 2.1.0 - 2018-03-20
### Changed/Fixed
- update 'getFlatMatched' of Match

## 2.0.0 - 2018-03-20
### Changed
- return map of matches instead of boolean
