const enum ValueType {
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

export { TEXT_TYPES, ValueType };

// No lambdas :(
