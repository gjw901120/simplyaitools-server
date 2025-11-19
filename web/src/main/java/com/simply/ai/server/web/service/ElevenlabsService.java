package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.*;

public interface ElevenlabsService {

    void elevenlabsTTS(ElevenlabsTTSDTO request);

    void elevenlabsSTT(ElevenlabsSTTDTO request);

    void elevenlabsAudioIsolationDTO(ElevenlabsAudioIsolationDTO request);

    void elevenlabsSoundEffectDTO(ElevenlabsSoundEffectDTO request);

}
