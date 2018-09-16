import { OperatorType } from '../../../common/model';
import { EqualityOperator } from './equality';

export class IsNotOperator extends EqualityOperator {
  public readonly type = OperatorType.IsNot;
}
