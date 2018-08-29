import { OperatorType } from '../../../common/model';
import { BinaryOperator } from './binary';

export class OrOperator extends BinaryOperator {
  public readonly type = OperatorType.Or;
}
