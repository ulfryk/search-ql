import { Map } from 'immutable';

import { OperatorAssociativity, OperatorType } from '../common/model';

const { And, Like, Not, NotLike, Gt, Gte, Lt, Lte, Is, IsNot, Or } = OperatorType;

const assignAssociativity = (...types: OperatorType[]) => (associativity: OperatorAssociativity) =>
  types.map(type => [type, associativity]);

// Order mostly taken from Java: https://introcs.cs.princeton.edu/java/11precedence/
export const operatorAssociativity = Map<OperatorType, OperatorAssociativity>([
  ...assignAssociativity(And, Or)(OperatorAssociativity.Left),
  ...assignAssociativity(Like, NotLike, Gt, Gte, Lt, Lte, Is, IsNot)(OperatorAssociativity.None),
  ...assignAssociativity(Not)(OperatorAssociativity.Right),
]);
