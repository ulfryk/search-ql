export const enum OperatorType {
  And = 'AND',
  Like = 'LIKE',
  Not = 'NOT',
  Or = 'OR',
  Gt = 'GT',
  Gte = 'GTE',
  Lt = 'LT',
  Lte = 'LTE',
  Is = 'IS',
  IsNot = 'IS NOT',
  NotLike = 'NOT LIKE',
}

export const allBinaryOperators = [
  OperatorType.And,
  OperatorType.Like,
  OperatorType.Or,
  OperatorType.Gt,
  OperatorType.Gte,
  OperatorType.Lt,
  OperatorType.Lte,
  OperatorType.Is,
  OperatorType.IsNot,
  OperatorType.NotLike,
];

export const allUnaryOperators = [
  OperatorType.Not,
];
