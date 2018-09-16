import { OperatorType } from '../../../common/model';

import { LogicalOperator } from './logical';

export class OrOperator extends LogicalOperator {
  public readonly type = OperatorType.Or;
}
