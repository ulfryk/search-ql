export enum ExpressionType {
  Function = 'expression:function',
  Binary = 'expression:binary-operation',
  Not = 'expression:not-operation', // Can be changed to unary-operation (with not-operator)
  Date = 'expression:date-term',
  Number = 'expression:number-term',
  Phrase = 'expression:phrase-term',
  Selector = 'expression:selector-term',
  Text = 'expression:text-term',
  // Boolean = 'expression:boolean-term',
  Invalid = 'INVALID',
}
