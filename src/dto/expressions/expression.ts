import { ExpressionType, ValueType } from '../../common/model';

export interface IExpression<V = any> {
  readonly returnType: ValueType;
  readonly type: ExpressionType;
  readonly value: V;
}
