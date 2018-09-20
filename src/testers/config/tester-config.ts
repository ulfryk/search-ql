import { Map } from 'immutable';

import { FunctionRuntime } from '../model';
import { builtInFunctions as allBuiltInFunctions } from '../runtimes/functions';

export class TesterConfig {

  public static create(
    { caseSensitive, builtInFunctions, customFunctions }: Partial<TesterConfig>,
  ) {
    return new TesterConfig(caseSensitive, builtInFunctions, customFunctions);
  }

  public readonly functions = this.builtInFunctions.concat(this.customFunctions).toMap();

  constructor(
    public readonly caseSensitive: boolean = false,
    public readonly builtInFunctions: Map<string, FunctionRuntime<any>> = allBuiltInFunctions,
    public readonly customFunctions: Map<string, FunctionRuntime<any>> = Map(),
  ) {}

}
