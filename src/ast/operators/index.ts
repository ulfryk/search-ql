import { Operator } from './operator';
import { tokenToOperator } from './token-to-operator';

Operator.fromToken = tokenToOperator;

export * from './binary';
export * from './unary';
export { Operator };
