import { OperatorType } from '../../config';

import { UnaryOperator } from './unary-operator';

export class NotOperator extends UnaryOperator {
  public static readonly one = new NotOperator();
  public readonly type = OperatorType.Not;
}
