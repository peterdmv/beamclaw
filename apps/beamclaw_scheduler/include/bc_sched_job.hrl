%%
%% Copyright Péter Dimitrov 2026, All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

%% Mnesia table record for bc_sched_jobs.
%%
%% Primary key is job_id (binary, UUID v4).
%% Table name is `bc_sched_jobs`; record tag is `bc_sched_job`.

-ifndef(BC_SCHED_JOB_HRL).
-define(BC_SCHED_JOB_HRL, true).

-record(bc_sched_job, {
    job_id         :: binary(),              %% PK, UUID v4
    agent_id       :: binary(),              %% which agent workspace
    user_id        :: binary(),              %% who created it

    %% Session linkage (for shared mode)
    session_id     :: binary() | undefined,  %% for shared mode; undefined = isolated

    %% Schedule
    schedule_type  :: at | every | random_in,
    schedule_spec  :: map(),
    %% at:        #{epoch_seconds => integer()}
    %% every:     #{interval_ms => integer()}
    %% random_in: #{interval_ms => integer(), count => integer()}

    %% Execution
    prompt         :: binary(),              %% message content for LLM
    session_mode   :: shared | isolated,
    autonomy       :: atom(),                %% autonomy_level()

    %% Delivery
    delivery       :: map(),
    %% #{channel => telegram | tui | webhook | silent,
    %%   chat_id => integer(),            %% Telegram-specific
    %%   webhook_url => binary()}         %% webhook-specific

    %% State
    status         :: active | paused | completed | failed,
    created_at     :: non_neg_integer(),     %% epoch seconds
    updated_at     :: non_neg_integer(),
    last_fired_at  :: non_neg_integer() | undefined,
    fire_count     :: non_neg_integer(),
    error_count    :: non_neg_integer(),     %% consecutive failures
    max_errors     :: non_neg_integer(),     %% auto-pause threshold (default 3)

    %% Heartbeat-specific
    heartbeat      :: boolean(),
    suppress_ok    :: boolean(),             %% suppress HEARTBEAT_OK output
    active_hours   :: {non_neg_integer(), non_neg_integer()} | undefined  %% {Start, End} UTC hour
}).

-endif. %% BC_SCHED_JOB_HRL
