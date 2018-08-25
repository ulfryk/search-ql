import { OperatorType } from '../../config';

import { BinaryOperator } from './binary-operator';

export class OrOperator extends BinaryOperator {
  public static readonly one = new OrOperator();
  public readonly type = OperatorType.Or;
}
