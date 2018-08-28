import { OperatorType } from '../../../config';

import { BinaryOperator } from './binary';

export class LikeOperator extends BinaryOperator {
  public readonly type = OperatorType.Like;
}
