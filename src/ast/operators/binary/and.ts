import { OperatorType } from '../../../common/model';
import { LogicalOperator } from './logical';

export class AndOperator extends LogicalOperator {
  public readonly type = OperatorType.And;
}
