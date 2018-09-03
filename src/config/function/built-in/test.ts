import { List, Map } from 'immutable';
import { None, Some } from 'monet';

import { Match, ValueType } from '../../../common/model';
import { OptionalFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

export const testFunction =
  new FunctionConfig(
    'test_function',
    List(),
    Some(OptionalFunctionArg.fromType(ValueType.Text, 'rest')),
    ValueType.Boolean,
      // tslint:disable-next-line:no-unnecessary-callback-wrapper
      () => None<Map<string, Match>>());
