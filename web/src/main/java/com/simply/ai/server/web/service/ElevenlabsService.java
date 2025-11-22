package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.*;
import com.simply.ai.server.web.model.dto.response.BaseResponse;

public interface ElevenlabsService {

    BaseResponse elevenlabsTTS(ElevenlabsTTSDTO request);

    BaseResponse elevenlabsSTT(ElevenlabsSTTDTO request);

    BaseResponse elevenlabsAudioIsolationDTO(ElevenlabsAudioIsolationDTO request);
    BaseResponse elevenlabsSoundEffectDTO(ElevenlabsSoundEffectDTO request);

}
