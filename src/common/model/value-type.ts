import { Map } from 'immutable';

const enum ValueType {
  Any = 'ANY',
  Boolean = 'BOOLEAN',
  Date = 'DATE',
  Number = 'NUMBER',
  Phrase = 'PHRASE',
  Text = 'TEXT',
}

const TEXT_TYPES: ReadonlyArray<ValueType> = [
  ValueType.Any,
  ValueType.Date,
  ValueType.Number,
  ValueType.Text,
];

const ANY_TYPES: ReadonlyArray<ValueType> = [
  ValueType.Boolean,
  ValueType.Phrase,
  ...TEXT_TYPES,
];

const isAnyType = (t: ValueType): boolean => ANY_TYPES.includes(t);
const isBooleanType = (t: ValueType): boolean => t === ValueType.Boolean;
const isDateType = (t: ValueType): boolean => t === ValueType.Date;
const isNumberType = (t: ValueType): boolean => t === ValueType.Number;
const isPhraseType = (t: ValueType): boolean => t === ValueType.Phrase;
const isTextType = (t: ValueType): boolean => TEXT_TYPES.includes(t);

const paramTypes = Map<ValueType, (t: ValueType) => boolean>({
  [ValueType.Any]: isAnyType,
  [ValueType.Boolean]: isBooleanType,
  [ValueType.Date]: isDateType,
  [ValueType.Number]: isNumberType,
  [ValueType.Phrase]: isPhraseType,
  [ValueType.Text]: isTextType,
});

const isSubtype = (t: ValueType, ofT: ValueType): boolean =>
  paramTypes.get(ofT)(t);

export {
  isAnyType,
  isBooleanType,
  isDateType,
  isNumberType,
  isPhraseType,
  isSubtype,
  isTextType,
  paramTypes,
  TEXT_TYPES,
  ValueType,
};

// No lambdas :(
