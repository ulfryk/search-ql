import { Operator } from './operator';
import { pojoToOperator } from './pojo-to-operator';
import { tokenToOperator } from './token-to-operator';

Operator.fromToken = tokenToOperator;
Operator.fromJS = pojoToOperator;

export * from './binary';
export * from './unary';
export { Operator };
