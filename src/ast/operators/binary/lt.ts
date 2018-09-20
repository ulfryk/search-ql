import { OperatorType } from '../../../common/model';
import { RelationalOperator } from './relational';

export class LtOperator extends RelationalOperator {
  public readonly type = OperatorType.Lt;
}
