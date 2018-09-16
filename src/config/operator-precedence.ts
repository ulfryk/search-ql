import { List, Map } from 'immutable';

import { OperatorType } from '../common/model';

// Order taken from Java: https://introcs.cs.princeton.edu/java/11precedence/
export const operatorPrecedence = Map<OperatorType, number>(List([
  [OperatorType.Or],
  [OperatorType.And],
  [OperatorType.Like, OperatorType.NotLike, OperatorType.Is, OperatorType.IsNot],
  [OperatorType.Gt, OperatorType.Gte, OperatorType.Lt, OperatorType.Lte],
  [OperatorType.Not],
]).flatMap((types, index) => types.map(type => [type, index + 1])));
