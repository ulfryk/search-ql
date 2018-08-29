import { OperatorType } from '../../../common/model';
import { BinaryOperator } from './binary';

export class LikeOperator extends BinaryOperator {
  public readonly type = OperatorType.Like;
}
