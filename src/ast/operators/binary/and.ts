import { OperatorType } from '../../../config';

import { BinaryOperator } from './binary';

export class AndOperator extends BinaryOperator {
  public static readonly one = new AndOperator();
  public readonly type = OperatorType.And;
}
