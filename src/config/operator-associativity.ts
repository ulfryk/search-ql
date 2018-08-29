import { Map } from 'immutable';

import { OperatorAssociativity, OperatorType } from '../common/model';

// Order taken from Java: https://introcs.cs.princeton.edu/java/11precedence/
export const operatorAssociativity = Map<OperatorType, OperatorAssociativity>([
  ...[OperatorType.And, OperatorType.Or].map(type => [type, OperatorAssociativity.Left]),
  [OperatorType.Like, OperatorAssociativity.None],
  [OperatorType.Not, OperatorAssociativity.Right],
]);
