import { OperatorType } from '../../../config';

import { UnaryOperator } from './unary';

export class NotOperator extends UnaryOperator {
  public static readonly one = new NotOperator();
  public readonly type = OperatorType.Not;
}
