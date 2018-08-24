import { OperatorType, SyntaxConfig } from '../../config';

import { AndOperator } from './and-operator';
import { NotOperator } from './not-operator';
import { Operator } from './operator';
import { OrOperator } from './or-operator';

export const tokenToOperator = (config: SyntaxConfig) => (token: string): Operator => {
  switch (config.getOperatorType(token)) {
    case OperatorType.And: return new AndOperator();
    case OperatorType.Not: return new NotOperator();
    case OperatorType.Or: return new OrOperator();
    default: throw Error(`Unknown OperatorType for token: "${token}"`);
  }
};
