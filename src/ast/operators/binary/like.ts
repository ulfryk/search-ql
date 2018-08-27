import { OperatorType } from '../../../config';

import { BinaryOperator } from './binary';

export class LikeOperator extends BinaryOperator {
  public static readonly one = new LikeOperator();
  public readonly type = OperatorType.Like;
}
