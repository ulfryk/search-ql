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

const isType = (a: string): a is ValueType => ANY_TYPES.includes(a as any);

const isSubtype = (t: ValueType, ofT: ValueType): boolean =>
  paramTypes.get(ofT)(t);

const isSupertype = (t: ValueType, ofT: ValueType): boolean =>
  paramTypes.get(t)(ofT);

const boolCompatible = [ValueType.Boolean, ValueType.Phrase];

const checkBoolCompatibility = (...types: ValueType[]) =>
  types.every(side => boolCompatible.includes(side));

export {
  checkBoolCompatibility,
  isAnyType,
  isBooleanType,
  isDateType,
  isNumberType,
  isPhraseType,
  isSubtype,
  isSupertype,
  isTextType,
  isType,
  paramTypes,
  TEXT_TYPES,
  ValueType,
};
