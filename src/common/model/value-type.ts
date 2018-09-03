import { Map } from 'immutable';

const enum ValueType {
  Any = 'ANY',
  Boolean = 'BOOLEAN',
  Date = 'DATE',
  Number = 'NUMBER',
  Text = 'TEXT',
}

const TEXT_TYPES: ReadonlyArray<ValueType> = [
  ValueType.Date,
  ValueType.Number,
  ValueType.Text,
];

const ANY_TYPES: ReadonlyArray<ValueType> = [
  ValueType.Any,
  ValueType.Boolean,
  ValueType.Date,
  ValueType.Number,
  ValueType.Text,
];

const isAnyType = (t: ValueType): boolean => ANY_TYPES.includes(t);
const isBooleanType = (t: ValueType): boolean => t === ValueType.Boolean;
const isDateType = (t: ValueType): boolean => t === ValueType.Date;
const isNumberType = (t: ValueType): boolean => t === ValueType.Number;
const isTextType = (t: ValueType): boolean => TEXT_TYPES.includes(t);

const paramTypes = Map<ValueType, (t: ValueType) => boolean>({
  [ValueType.Any]: isAnyType,
  [ValueType.Boolean]: isBooleanType,
  [ValueType.Date]: isDateType,
  [ValueType.Number]: isNumberType,
  [ValueType.Text]: isTextType,
});

export {
  isBooleanType,
  isDateType,
  isNumberType,
  isTextType,
  paramTypes,
  TEXT_TYPES,
  ValueType,
};

// No lambdas :(
