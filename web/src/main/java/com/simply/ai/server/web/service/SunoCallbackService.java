package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.callback.suno.SunoExtendRequest;
import com.simply.ai.server.web.model.dto.request.callback.suno.SunoGenerateRequest;
import com.simply.ai.server.web.model.dto.request.callback.suno.SunoUploadCoverRequest;
import com.simply.ai.server.web.model.dto.request.callback.suno.SunoUploadExtendRequest;
import com.simply.ai.server.web.model.dto.request.callback.suno.SunoAddInstrumentalRequest;
import com.simply.ai.server.web.model.dto.request.callback.suno.SunoAddVocalsRequest;

public interface SunoCallbackService {

    void generateCallback(SunoGenerateRequest request);

    void extendCallback(SunoExtendRequest request);

    void uploadCoverCallback(SunoUploadCoverRequest request);

    void uploadExtendCallback(SunoUploadExtendRequest request);

    void addInstrumentalCallback(SunoAddInstrumentalRequest request);

    void addVocalsCallback(SunoAddVocalsRequest request);
}