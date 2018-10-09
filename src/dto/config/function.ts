import { ValueType } from '../../common/model';
import { IFunctionArg } from './function-arg';

export interface IFunctionConfig {
  readonly aliases?: string[];
  readonly args: IFunctionArg[];
  readonly argsRest: IFunctionArg | null;
  readonly name: string;
  readonly returnType: ValueType | string;
}
