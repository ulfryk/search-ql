export enum ExpressionType {
  Function = 'function',
  Binary = 'binary-operation',
  Not = 'not-operation', // Can be changed to unary-operation (with not-operator)
  Date = 'date-term',
  Number = 'number-term',
  Phrase = 'phrase-term',
  Selector = 'selector-term',
  Text = 'text-term',
  // Boolean = 'boolean-term',
  Invalid = 'INVALID',
}
