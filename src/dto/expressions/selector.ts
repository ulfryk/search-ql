import { ValueType } from '../../common/model';
import { ITermExpression } from './term';

export interface ISelectorExpression extends ITermExpression<string> {
  readonly matchingType: ValueType;
}
