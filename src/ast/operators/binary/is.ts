import { OperatorType } from '../../../common/model';
import { EqualityOperator } from './equality';

export class IsOperator extends EqualityOperator {
  public readonly type = OperatorType.Is;
}
