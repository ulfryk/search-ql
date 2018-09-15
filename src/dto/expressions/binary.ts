import { IOperator } from '../operator';
import { IExpression } from './expression';

export interface IBinaryOperationExpression<T extends IExpression, X extends IExpression>
extends IExpression<[T, X]> {
  readonly operator: IOperator;
}
