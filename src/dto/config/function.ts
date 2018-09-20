import { ValueType } from '../../common/model';
import { IFunctionArg } from './function-arg';

export interface IFunctionConfig {
  readonly name: string;
  readonly args: IFunctionArg[];
  readonly argsRest: IFunctionArg | null;
  readonly returnType: ValueType;
}
