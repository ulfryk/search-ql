import { OperatorType } from '../../../config';

import { BinaryOperator } from './binary';

export class OrOperator extends BinaryOperator {
  public static readonly one = new OrOperator();
  public readonly type = OperatorType.Or;
}
