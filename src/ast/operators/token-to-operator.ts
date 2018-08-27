import { OperatorType, SyntaxConfig } from '../../config';

import { AndOperator, OrOperator } from './binary';
import { Operator } from './operator';
import { NotOperator } from './unary';

export const tokenToOperator = (config: SyntaxConfig) => (token: string): Operator => {
  switch (config.getOperatorType(token)) {
    case OperatorType.And: return new AndOperator(token);
    case OperatorType.Not: return new NotOperator(token);
    case OperatorType.Or: return new OrOperator(token);
    default: throw Error(`Unknown OperatorType for token: "${token}"`);
  }
};
