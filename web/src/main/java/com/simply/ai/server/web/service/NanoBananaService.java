package com.simply.ai.server.web.service;

import com.simply.ai.server.web.model.dto.request.NanoBananaEditDTO;
import com.simply.ai.server.web.model.dto.request.NanoBananaGenerateDTO;

public interface NanoBananaService {

    void nanoBananaGenerate(NanoBananaGenerateDTO request);

    void nanoBananaEdit(NanoBananaEditDTO request);

}
