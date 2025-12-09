package com.simply.ai.server.manager.manager;

import com.simply.ai.server.manager.model.request.DeepseekRequest;
import com.simply.ai.server.manager.model.response.DeepseekResponse;
import reactor.core.publisher.Flux;

public interface DeepseekConversionManager {

    Flux<DeepseekResponse> streamChat(DeepseekRequest request);
}
