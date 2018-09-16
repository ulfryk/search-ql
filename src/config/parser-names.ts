enum ParserName {
  Basic = 'basic-expression',
  BinaryOperation = 'binary-operation-expression', // TODO: Limit operators
  Function = 'function-expression',
  Not = 'not-expression', // TODO: change to unary-operator and limit operators
}

const allParsers = [
  ParserName.Basic,
  ParserName.BinaryOperation,
  ParserName.Function,
  ParserName.Not,
];

export { ParserName, allParsers };
