import { OperatorType, SyntaxConfig } from '../../config';

import { AndOperator } from './and-operator';
import { NotOperator } from './not-operator';
import { Operator } from './operator';
import { OrOperator } from './or-operator';

export const tokenToOperator = (config: SyntaxConfig) => (token: string): Operator => {
  switch (config.getOperatorType(token)) {
    case OperatorType.And: return new AndOperator(token);
    case OperatorType.Not: return new NotOperator(token);
    case OperatorType.Or: return new OrOperator(token);
    default: throw Error(`Unknown OperatorType for token: "${token}"`);
  }
};
