import { Map } from 'immutable';

import { OperatorAssociativity, OperatorType } from '../common/model';

const assignAssociativity = (...types: OperatorType[]) => (associativity: OperatorAssociativity) =>
  types.map(type => [type, associativity]);

// Order mostly taken from Java: https://introcs.cs.princeton.edu/java/11precedence/
export const operatorAssociativity = Map<OperatorType, OperatorAssociativity>([
  ...assignAssociativity(
    OperatorType.And, OperatorType.Or, OperatorType.Like, OperatorType.NotLike, OperatorType.Is,
    OperatorType.IsNot)(OperatorAssociativity.Left),
  ...assignAssociativity(
    OperatorType.Gt, OperatorType.Gte, OperatorType.Lt, OperatorType.Lte)(
      OperatorAssociativity.None),
  ...assignAssociativity(OperatorType.Not)(OperatorAssociativity.Right),
]);
