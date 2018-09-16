import { OperatorType } from '../../../common/model';
import { EqualityOperator } from './equality';

export class LikeOperator extends EqualityOperator {
  public readonly type = OperatorType.Like;
}
