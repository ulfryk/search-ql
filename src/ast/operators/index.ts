import { Operator } from './operator';
import { pojoToOperator } from './pojo-to-operator';
import { tokenToOperator } from './token-to-operator';

// tslint:disable-next-line:no-object-mutation
Operator.fromToken = tokenToOperator;

// tslint:disable-next-line:no-object-mutation
Operator.fromJS = pojoToOperator;

export * from './binary';
export * from './unary';
export { Operator };
