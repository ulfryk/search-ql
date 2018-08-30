import { OperatorType } from '../../../common/model';
import { BinaryOperator } from './binary';

export class AndOperator extends BinaryOperator {
  public readonly type = OperatorType.And;
}
