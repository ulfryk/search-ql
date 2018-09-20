import { List } from 'immutable';
import { Some } from 'monet';

import { ValueType } from '../../../common/model';
import { OptionalFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

export const testFunction =
  new FunctionConfig(
    'test_function',
    List(),
    Some(OptionalFunctionArg.fromType(ValueType.Text, 'rest')),
    ValueType.Boolean);
