import { OperatorType } from '../../../common/model';
import { EqualityOperator } from './equality';

export class NotLikeOperator extends EqualityOperator {
  public readonly type: OperatorType = OperatorType.NotLike;
}
