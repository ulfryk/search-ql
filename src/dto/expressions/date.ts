import { ITimeFrame } from '../time-frame';
import { ITermExpression } from './term';

export interface IDateExpression extends ITermExpression {
  readonly timeFrame: ITimeFrame;
}
