import { IOperator } from '../operator';
import { IExpression } from './expression';

export interface IBinaryOperationExpression extends IExpression<IExpression[]> {
  readonly operator: IOperator;
}
