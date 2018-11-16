import { OperatorType } from '../../../common/model';
import { RelationalOperator } from './relational';

export class GtOperator extends RelationalOperator {
  public readonly type: OperatorType = OperatorType.Gt;
}
