package com.simply.ai.server.web.service;

import com.simply.ai.server.manager.model.response.ChatgptResponse;
import com.simply.ai.server.manager.model.response.ClaudeResponse;
import com.simply.ai.server.manager.model.response.DeepseekResponse;
import com.simply.ai.server.web.model.dto.request.chat.ChatgptConversionDTO;
import com.simply.ai.server.web.model.dto.request.chat.ClaudeConversionDTO;
import com.simply.ai.server.web.model.dto.request.chat.DeepseekConversionDTO;
import reactor.core.publisher.Flux;

public interface ChatConversionService {

    Flux<DeepseekResponse> processDeepseekStream(DeepseekConversionDTO request);

    Flux<ChatgptResponse> processChatgptStream(ChatgptConversionDTO request);

    /**
     * 处理Claude流式响应并聚合内容
     */
    // Claude新增方法 - 只保留流式
    Flux<ClaudeResponse.AggregatedEvent> processClaudeStream(ClaudeConversionDTO request);// Claude新增方法 - 只保留流式

}
